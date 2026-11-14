module ContextFilterSpec (contextFilterTests) where

import CGrep.Boundary (Boundary (..), BoundaryGuard (..))
import CGrep.FileType (FileType (..))
import CGrep.Parser.Chunk (
    ChunkType,
    pattern ChunkIdentifier,
    pattern ChunkKeyword,
    pattern ChunkOperator,
    pattern ChunkString,
 )
import CGrep.Parser.Token (tOffset, tToken, tTyp)
import CGrep.Semantic.ContextFilter (
    contextBitCode,
    contextBitComment,
    contextBitLiteral,
    contextFilterAll,
    mkParConfig,
    runContextFilter,
    (~!),
 )
import CGrep.Text (textOffsetWord8)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import Options (Options (..), defaultOptions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TestHelpers (filterSource, lexSource, matchLineNums, matchLines, runSearchMem)

contextFilterTests :: TestTree
contextFilterTests =
    testGroup
        "ContextFilter corner cases"
        [ rawVsLiteralPriorityTests
        , boundaryLengthSortingTests
        , multiCharDelimiterMarkerTests
        , contextFlagCombinationTests
        , degenerateBoundaryTests
        , utf8ByteLengthInvarianceTests
        ]

rawVsLiteralPriorityTests :: TestTree
rawVsLiteralPriorityTests =
    testGroup
        "rawBound takes priority over litrBound (triple-quoted strings)"
        [ testCase "Python triple double-quoted string with inner unescaped quote does not swallow code" $ do
            let src = "doc = \"\"\"5\" inch\"\"\"\ntarget = 42\n"
                opt = defaultOptions{semantic = True, code = True}
            ms <- runSearchMem opt Python "sample.py" src ["target = 42"]
            matchLineNums ms @?= [2]
        , testCase "Python triple single-quoted string with inner apostrophe does not swallow code" $ do
            let src = "doc = '''it's fine'''\ntarget = 99\n"
                opt = defaultOptions{semantic = True, code = True}
            ms <- runSearchMem opt Python "sample.py" src ["target = 99"]
            matchLineNums ms @?= [2]
        , testCase "Python code-only (-c) strips identifier inside triple-quoted docstring" $ do
            let src = "\"\"\"\ndef hidden_fn(): pass\n\"\"\"\ndef visible_fn(): pass\n"
                opt = defaultOptions{semantic = True, code = True}
            msHidden <- runSearchMem opt Python "sample.py" src ["hidden_fn"]
            msVisible <- runSearchMem opt Python "sample.py" src ["visible_fn"]
            matchLineNums msHidden @?= []
            matchLineNums msVisible @?= [4]
        , testCase "Kotlin, Swift, Chapel, Scala triple-quoted raw strings preserve subsequent code" $ do
            let checkLang ft fname src pat expectedLine = do
                    let opt = defaultOptions{semantic = True, code = True}
                    ms <- runSearchMem opt ft fname src [pat]
                    matchLineNums ms @?= [expectedLine]
            checkLang Kotlin "a.kt" "val s = \"\"\"a \"b\" c\"\"\"\nval target = 1\n" "val target = 1" 2
            checkLang Swift "a.swift" "let s = \"\"\"a \"b\" c\"\"\"\nlet target = 1\n" "let target = 1" 2
            checkLang Chapel "a.chpl" "var s = \"\"\"a \"b\" c\"\"\";\nvar target = 1;\n" "var target = 1" 2
            checkLang Scala "a.scala" "val s = \"\"\"a \"b\" c\"\"\"\nval target = 1\n" "val target = 1" 2
        , testCase "Triple-quoted strings ending with 4, 5, or 6 consecutive quotes do not leak unclosed quotes" $ do
            -- 4 quotes at end: 1 inner quote + closing """
            let src4 = "val a = \"\"\"hello \"world\"\"\"\"\nval target4 = 1\n"
                toks4 = lexSource Kotlin defaultOptions False src4
            [tToken t | t <- toks4, tTyp t == ChunkString] @?= ["hello \"world\""]
            ms4 <- runSearchMem defaultOptions{semantic = True, code = True} Kotlin "a.kt" src4 ["val target4 = 1"]
            matchLineNums ms4 @?= [2]
            -- 5 quotes at end: 2 inner quotes + closing """
            let src5 = "val b = \"\"\"x \"\"\"\"\"\nval target5 = 2\n"
            ms5 <- runSearchMem defaultOptions{semantic = True, code = True} Kotlin "a.kt" src5 ["val target5 = 2"]
            matchLineNums ms5 @?= [2]
            -- 6 quotes: empty triple-quoted string """"""
            let src6 = "val c = \"\"\"\"\"\"\nval target6 = 3\n"
                toks6 = lexSource Kotlin defaultOptions False src6
            [tToken t | t <- toks6, tTyp t == ChunkString] @?= [""]
            ms6 <- runSearchMem defaultOptions{semantic = True, code = True} Kotlin "a.kt" src6 ["val target6 = 3"]
            matchLineNums ms6 @?= [2]
        ]

boundaryLengthSortingTests :: TestTree
boundaryLengthSortingTests =
    testGroup
        "Intra-list boundary sorting (longest delimiter matched first)"
        [ testCase "CoffeeScript ### block comment is not shadowed by # line comment" $ do
            let src = "###\nclass FakeInComment\n###\nclass RealClass\n"
                optCode = defaultOptions{semantic = True, code = True}
                optComm = defaultOptions{comment = True}
            msFake <- runSearchMem optCode Coffee "a.coffee" src ["FakeInComment"]
            msReal <- runSearchMem optCode Coffee "a.coffee" src ["RealClass"]
            msComm <- runSearchMem optComm Coffee "a.coffee" src ["FakeInComment"]
            matchLineNums msFake @?= []
            matchLineNums msReal @?= [4]
            matchLineNums msComm @?= [2]
        , testCase "Julia #- ... -# multi-line comment is not shadowed by # line comment" $ do
            let src = "#-\nhidden_sym = 1\n-#\nvisible_sym = 2\n"
                optCode = defaultOptions{semantic = True, code = True}
            msHidden <- runSearchMem optCode Julia "a.jl" src ["hidden_sym"]
            msVisible <- runSearchMem optCode Julia "a.jl" src ["visible_sym"]
            matchLineNums msHidden @?= []
            matchLineNums msVisible @?= [4]
        , testCase "TOML \"\"\" and ''' multi-line strings are not shadowed by \" and '" $ do
            let src = "a = \"\"\"hello \"world\"\nsecret_code = 1\"\"\"\nreal_key = 2\n"
                optCode = defaultOptions{code = True}
            msSecret <- runSearchMem optCode Toml "a.toml" src ["secret_code"]
            msReal <- runSearchMem optCode Toml "a.toml" src ["real_key"]
            matchLineNums msSecret @?= []
            matchLineNums msReal @?= [3]
        , testCase "Rust r##\"...\"## is not prematurely closed by inner \"#" $ do
            let src = "let s = r##\"inner \"# end\"##;\nlet after = 1;\n"
                optCode = defaultOptions{semantic = True, code = True}
            msInner <- runSearchMem optCode Rust "a.rs" src ["end"]
            msAfter <- runSearchMem optCode Rust "a.rs" src ["let after = 1"]
            matchLineNums msInner @?= []
            matchLineNums msAfter @?= [2]
        ]

multiCharDelimiterMarkerTests :: TestTree
multiCharDelimiterMarkerTests =
    testGroup
        "Multi-character delimiters do not leak spurious code tokens"
        [ testCase "Rust r#\"hello\"# emits only let, s, =, string, ; (no leaked r or #)" $ do
            let src = "let s = r#\"hello\"#;"
                toks = lexSource Rust defaultOptions False src
                pairs :: [(ChunkType, T.Text)]
                pairs = map (\t -> (tTyp t, tToken t)) toks
            pairs
                @?= [ (ChunkKeyword, "let")
                    , (ChunkIdentifier, "s")
                    , (ChunkOperator, "=")
                    , (ChunkString, "hello")
                    , (ChunkOperator, ";")
                    ]
        , testCase "C++ R\"(hello)\" and R\"--(a)\"b)--\" do not leak R identifier or unclosed quote" $ do
            let src = "auto a = R\"(hello)\";\nauto b = R\"--(x)\"y)--\";\nint target = 7;\n"
                opt = defaultOptions{semantic = True}
            msR <- runSearchMem opt Cpp "a.cpp" src ["R"]
            msTarget <- runSearchMem opt Cpp "a.cpp" src ["int target = 7"]
            msStr <- runSearchMem opt Cpp "a.cpp" src ["auto a = STR"]
            matchLineNums msR @?= []
            matchLineNums msTarget @?= [3]
            matchLineNums msStr @?= [1]
        , testCase "Lua [===[raw]===] and [[multi]] emit clean string tokens without leaked [ or =" $ do
            let src = "local x = [===[raw content]===]\nlocal y = 10\n"
                opt = defaultOptions{semantic = True}
            msStr <- runSearchMem opt Lua "a.lua" src ["local x = STR"]
            msY <- runSearchMem opt Lua "a.lua" src ["local y = 10"]
            matchLineNums msStr @?= [1]
            matchLineNums msY @?= [2]
        , testCase "Haskell [here|...|] quasi-quote raw string does not leak identifier 'here'" $ do
            let src = "msg = [here|some text|]\nval = 42\n"
                opt = defaultOptions{semantic = True}
            msHere <- runSearchMem opt Haskell "a.hs" src ["here"]
            msMsg <- runSearchMem opt Haskell "a.hs" src ["msg = STR"]
            msVal <- runSearchMem opt Haskell "a.hs" src ["val = 42"]
            matchLineNums msHere @?= []
            matchLineNums msMsg @?= [1]
            matchLineNums msVal @?= [2]
        , testCase "Go raw string ending in backslash (`path\\`) closes cleanly on backtick" $ do
            let src = "p := `C:\\dir\\`\ntarget := 1\n"
                opt = defaultOptions{semantic = True, code = True}
            ms <- runSearchMem opt Go "a.go" src ["target := 1"]
            matchLineNums ms @?= [2]
        , testCase "Escaped backslashes vs escaped quotes in normal strings (\"abc\\\\\" vs \"abc\\\"def\")" $ do
            let src = "const char *a = \"abc\\\\\";\nint between = 1;\nconst char *b = \"abc\\\"between\";\nint after = 2;\n"
                opt = defaultOptions{semantic = True, code = True}
            msBetween <- runSearchMem opt C "a.c" src ["between"]
            msAfter <- runSearchMem opt C "a.c" src ["int after = 2"]
            matchLineNums msBetween @?= [2]
            matchLineNums msAfter @?= [4]
        , testCase "Multi-char escaped char literals ('\\x41', '\\u0041', '\\u{1F980}', '\\'', '\\\\') vs Rust lifetimes ('a, 'static)" $ do
            let rustSrc =
                    T.unlines
                        [ "fn pick<'a>(s: &'static str) -> char {"
                        , "    let c1 = '\\x41';"
                        , "    let c2 = '\\u{1F980}';"
                        , "    let c3 = '\\'';"
                        , "    let c4 = '\\\\';"
                        , "    c1"
                        , "}"
                        ]
            msLit <- runSearchMem defaultOptions{semantic = True} Rust "a.rs" rustSrc ["let _ = LIT"]
            matchLineNums msLit @?= [2, 3, 4, 5]
            -- Code-only (-c) must NOT leak 'x41' as an identifier inside '\x41'
            msLeak <- runSearchMem defaultOptions{semantic = True, code = True} Rust "a.rs" rustSrc ["x41"]
            matchLineNums msLeak @?= []
            -- Lifetimes 'a and 'static must not swallow 'c1' return expression on line 6
            msRet <- runSearchMem defaultOptions{semantic = True, code = True, word_match = True} Rust "a.rs" rustSrc ["c1"]
            matchLineNums msRet @?= [2, 6]
        , testCase "Haskell primed identifiers (x', y') coexist with char literals ('a', '\\NUL')" $ do
            let hsSrc = "calc x' y' = if x' == '\\NUL' then 'a' else y'\n"
            msPrimed <- runSearchMem defaultOptions{semantic = True, code = True, word_match = True} Haskell "a.hs" hsSrc ["x'"]
            msLit <- runSearchMem defaultOptions{semantic = True} Haskell "a.hs" hsSrc ["x' == LIT"]
            matchLineNums msPrimed @?= [1]
            matchLineNums msLit @?= [1]
        ]

contextFlagCombinationTests :: TestTree
contextFlagCombinationTests =
    testGroup
        "Context filter flag combinations (-l, -c, -m) with literal markers"
        [ testCase "Literal-only (-l) with --string tokenizer matches string literals and ignores code" $ do
            let src = "let hello = \"world\";\n"
                optStrLit = defaultOptions{string = True, literal = True}
            msWorld <- runSearchMem optStrLit Rust "a.rs" src ["world"]
            msHello <- runSearchMem optStrLit Rust "a.rs" src ["hello"]
            matchLineNums msWorld @?= [1]
            matchLineNums msHello @?= []
        , testCase "Literal-only (-l) with semantic (-S) matches STR and exact string literal" $ do
            let src = "const char *msg = \"needle\";\nint needle = 0;\n"
                optSemLit = defaultOptions{semantic = True, literal = True}
            msWild <- runSearchMem optSemLit C "a.c" src ["STR"]
            msExact <- runSearchMem optSemLit C "a.c" src ["\"needle\""]
            msIdent <- runSearchMem optSemLit C "a.c" src ["needle"]
            matchLineNums msWild @?= [1]
            matchLineNums msExact @?= [1]
            matchLineNums msIdent @?= []
        , testCase "Code-only (-c) with semantic (-S) blanks string contents but preserves STR token slot" $ do
            let src = "f(\"secret_payload\");\n"
                optSemCode = defaultOptions{semantic = True, code = True}
            msCall <- runSearchMem optSemCode C "a.c" src ["f(STR)"]
            msSecret <- runSearchMem optSemCode C "a.c" src ["\"secret_payload\""]
            matchLineNums msCall @?= [1]
            matchLineNums msSecret @?= []
        , testCase "Comment-only (-m) ignores comment-like sequences inside string literals" $ do
            let src = "url = \"http://not-a-comment\"\n// real-comment\n"
                optComm = defaultOptions{comment = True}
            msFake <- runSearchMem optComm C "a.c" src ["not-a-comment"]
            msReal <- runSearchMem optComm C "a.c" src ["real-comment"]
            matchLineNums msFake @?= []
            matchLines msReal @?= ["// real-comment"]
        ]

degenerateBoundaryTests :: TestTree
degenerateBoundaryTests =
    testGroup
        "Degenerate and empty boundary safety"
        [ testCase "SmallTalk empty end boundary (\"$\" ~~ \"\") does not crash ContextFilter" $ do
            let src = "\"a smalltalk comment\"\nx := $a.\ny := 'hello'.\n"
                filtered = filterSource SmallTalk defaultOptions{code = True} True src
            TU.lengthWord8 filtered @?= TU.lengthWord8 src
            ms <- runSearchMem defaultOptions{semantic = True, code = True} SmallTalk "a.st" src ["y := STR"]
            matchLineNums ms @?= [3]
        , testCase "Direct mkParConfig with empty begin or end boundary is total" $ do
            let badBounds = [Boundary "" 0 "\"" 1 NoBoundaryGuard, Boundary "$" 1 "" 0 NoBoundaryGuard, Boundary "" 0 "" 0 NoBoundaryGuard]
                cfg = mkParConfig badBounds badBounds badBounds badBounds True
                out = runContextFilter cfg (contextFilterAll ~! contextBitComment) "abc $x def"
            out @?= "abc $x def"
        , testCase "Empty input text returns empty text across all context filters" $ do
            let cfg = mkParConfig [Boundary "//" 2 "\n" 1 NoBoundaryGuard] [Boundary "\"" 1 "\"" 1 NoBoundaryGuard] [] [] True
            runContextFilter cfg contextFilterAll "" @?= ""
            runContextFilter cfg (contextFilterAll ~! contextBitLiteral) "" @?= ""
            runContextFilter cfg (contextFilterAll ~! contextBitCode) "" @?= ""
        ]

utf8ByteLengthInvarianceTests :: TestTree
utf8ByteLengthInvarianceTests =
    testGroup
        "UTF-8 byte-length and byte-offset preservation"
        [ testCase "ContextFilter preserves exact Word8 length across 1/2/3/4-byte UTF-8 sequences" $ do
            let src =
                    T.unlines
                        [ "// ASCII, 2-byte © αβ, 3-byte € 日本語, 4-byte 🦀 🚀"
                        , "const char *s = \"© € 語 🦀\";"
                        , "auto r = R\"(raw © € 語 🦀)\";"
                        , "int after_utf8 = 123;"
                        ]
                checkOpts o useMarkers = do
                    let out = filterSource Cpp o useMarkers src
                    TU.lengthWord8 out @?= TU.lengthWord8 src
            checkOpts defaultOptions{code = True} False
            checkOpts defaultOptions{code = True} True
            checkOpts defaultOptions{comment = True} False
            checkOpts defaultOptions{literal = True} False
            checkOpts defaultOptions{literal = True} True
        , testCase "Tokens following multi-byte comments/strings retain exact byte offsets and emit zero phantom operators" $ do
            let src = "/* © € 語 🦀 */\nlet target = \"αβγ\";\n"
                toks = lexSource Rust defaultOptions{code = True} False src
                -- Must NOT contain any phantom ChunkOperator '\x3000' or '\x100000' from blanked 3/4-byte UTF-8!
                tokTypes = map tTyp toks
            tokTypes @?= [ChunkKeyword, ChunkIdentifier, ChunkOperator, ChunkString, ChunkOperator]
            let identToks = [t | t <- toks, tToken t == "target"]
            length identToks @?= 1
            case identToks of
                (t : _) -> do
                    let off = tOffset t
                        sliced = TU.takeWord8 (TU.lengthWord8 "target") (TU.dropWord8 off src)
                    assertBool "offset must be non-negative" (off >= 0)
                    textOffsetWord8 (tToken t) @?= off
                    sliced @?= "target"
                [] -> assertBool "expected target token" False
        , testCase "Blanked 3/4-byte UTF-8 comments inside expressions (foo(/* € — 🦀 */)) and #[test] // € do not break matching" $ do
            let cSrc = "void run() { foo(/* € — 🦀 */); }\n"
            msCall <- runSearchMem defaultOptions{semantic = True} C "a.c" cSrc ["foo()"]
            matchLineNums msCall @?= [1]
            let rsSrc = "fn prod() { check(); }\n#[test] // € — 🦀\nfn t() { check(); }\n"
            msTest <- runSearchMem defaultOptions{semantic = True, tests = Just True} Rust "a.rs" rsSrc ["check()"]
            msProd <- runSearchMem defaultOptions{semantic = True, tests = Just False} Rust "a.rs" rsSrc ["check()"]
            matchLineNums msTest @?= [3]
            matchLineNums msProd @?= [1]
        ]
