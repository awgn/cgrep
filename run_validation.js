const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const dir = './test_suite';
const files = fs.readdirSync(dir).filter(f => fs.statSync(path.join(dir, f)).isFile());

let all_success = true;

for (const file of files) {
  const filePath = path.join(dir, file);
  console.log(`\n=====================================`);
  console.log(`Testing: ${filePath}`);

  const runTest = (name, cmd, expectedType) => {
    let out = "";
    try {
      out = execSync(`${cmd} | grep cgrep_ || true`, { encoding: 'utf8' });
    } catch (e) {
      out = "";
    }
    
    const lines = out.trim().split('\n').filter(l => l.length > 0);
    
    let expectedNum = 1;
    let passed = true;
    let details = [];

    for (const line of lines) {
      // Find cgrep_(prod|test)_(\d+)
      const match = line.match(/cgrep_(prod|test)_(\d+)/);
      if (!match) {
        details.push(`UNEXPECTED LINE: ${line}`);
        passed = false;
        continue;
      }
      const type = match[1];
      const num = parseInt(match[2], 10);
      
      if (type !== expectedType) {
        details.push(`WRONG TYPE: found ${type}, expected ${expectedType} in ${line}`);
        passed = false;
      }
      if (num !== expectedNum) {
        details.push(`WRONG NUM: found ${num}, expected ${expectedNum} in ${line}`);
        passed = false;
      }
      expectedNum++;
    }

    if (lines.length === 0) {
      details.push("NO OUTPUT");
      passed = false;
    }

    if (passed) {
      console.log(`  ✅ ${name} (${lines.length} items)`);
    } else {
      console.log(`  ❌ ${name}`);
      details.forEach(d => console.log(`       ${d}`));
      all_success = false;
    }
  };

  runTest("Prod (-T False)", `stack run -- -S cgrep_ -T False ${filePath}`, "prod");
  runTest("Test (-T True)", `stack run -- -S cgrep_ -T True ${filePath}`, "test");
  runTest("Prod Strict (--strict -T False)", `stack run -- --strict -S cgrep_ -T False ${filePath}`, "prod");
  runTest("Test Strict (--strict -T True)", `stack run -- --strict -S cgrep_ -T True ${filePath}`, "test");
}

if (all_success) {
  console.log("\n✅ ALL TESTS PASSED SUCCESSFULLY! ✅");
} else {
  console.log("\n❌ SOME TESTS FAILED! ❌");
}
