## Changelog

# cgrep v8.0.0

- **Performance Enhancements:** In this release, significant improvements have been made to enhance the performance of cgrep. The performance is now more on par with ripgrep, with search speeds ranging from 3 times to 13 times faster than the previous release.

- **Expanded Language and Configuration Support:** We have expanded the language and configuration support in cgrep. Starting from this version, it includes support for searching within Zig, Union, Dhall, Fish shell, Toml, and Ini config files. This means you can now easily search for patterns and text within files of these formats.

- **New Semantic Token Filter:** A new semantic token filter has been introduced in this release. This filter is designed to identify and filter out specific native types present in different file types. This enhances the search results by providing more precise and relevant matches based on the semantic meaning of the tokens.

- **Kind Filter Selector:** We have introduced a new feature called the Kind Filter Selector. With this filter selector, users can now specify the kind of files they want to search within. The available options for the filter include Text, Config, Language, Data, Markup, and Script. This allows for more focused and targeted searches based on the desired file type, especially useful in large codebases.
