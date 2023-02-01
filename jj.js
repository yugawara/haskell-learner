const fs = require('fs');

fs.readFile('./package.json', 'utf-8', (err, data) => {
    if (err) throw err;

    const packageJson = JSON.parse(data);

    let report = `
    <html>
      <head>
        <title>Package.json Report</title>
      </head>
      <body>
        <h1>Package.json Report</h1>
        <p>The following information was extracted from your package.json file:</p>
        <ul>
          <li><strong>Name:</strong> ${packageJson.name}</li>
          <li><strong>Version:</strong> ${packageJson.version}</li>
          <li><strong>Description:</strong> ${packageJson.description}</li>
          <li><strong>Main:</strong> ${packageJson.main}</li>
          <li><strong>Scripts:</strong>
            <ul>
  `;

    for (const script in packageJson.scripts) {
        report += `<li>${script}: ${packageJson.scripts[script]}</li>`;
    }

    report += `
            </ul>
          </li>
          <li><strong>Dependencies:</strong>
            <ul>
  `;

    const dependencies = { ...packageJson.dependencies, ...packageJson.devDependencies };
    for (const dependency in dependencies) {
        const version = dependencies[dependency];
        let meaning = '';
        let meaning2 = '';
        if (version.startsWith('^')) {
            meaning = 'The caret (^) symbol in the version number means that you can use a version that is compatible with the specified version. The caret ensures that you are using the latest version that is backwards compatible with the specified version.';
        } else if (version.startsWith('~')) {
            meaning = 'The tilde (~) symbol in the version number means that you can use a version that is compatible with the specified version, but not necessarily the latest version. The tilde allows you to use the latest patch version (the third number in the version) of the specified version.';
        } else {
            meaning = 'The exact version number means that you must use the exact version specified, no newer or older versions are allowed.';
        }
        const versionNumbers = version.split(".");
        if (versionNumbers.length === 3) {
            meaning2 += `
                <ul>
                  <li>Major version: ${versionNumbers[0]}</li>
                  <li> Minor version: ${versionNumbers[1]}</li>
                  <li> Patch version: ${versionNumbers[2]}</li>
                </ul>
    `;
        }

        if (dependency.startsWith('@')) {
            meaning3 = 'The `@` symbol in the version number specifies the version of a scoped package. Scoped packages are packages that are prefixed with an `@` symbol, and they are usually owned by a specific organization or user.';
        } else {
            meaning3 = ''
        }
        report += `<li>${dependency}: ${version}<br><strong>Interpreted Meaning:</strong> ${meaning}
   <br><strong>Interpreted Meaning:</strong> ${meaning2} </li>
   <br><strong>Interpreted Meaning:</strong> ${meaning3} </li>
   `;
    }

    report += `
            </ul>
          </li>
        </ul>
      </body>
    </html>
  `;
    fs.writeFile('./report.html', report, (err) => {
        if (err) throw err;
        console.log('Report generated successfully!');
    });
})