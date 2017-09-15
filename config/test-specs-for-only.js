const fs = require('fs');
const { Identity, Some } = require('monet');

const ROOT = 'src';
const FILE_PATTERN = '.spec.';
const FORBIDDEN_PATTERNS = ['describe.only', 'it.only'];
const SUCCESS_MESSAGE = `No use of ${FORBIDDEN_PATTERNS.map(warning => `"${warning}"`).join(' or ')} found in spec files :)`;

const getWarning = line => pattern => line.includes(pattern) ? pattern : null;

const getLineWarnings = line => FORBIDDEN_PATTERNS.map(getWarning(line)).filter(Boolean);

const getLineInfo = file => (line, index) => ({ file, line, index, warnings: getLineWarnings(line) });

const getFileNames = path => fs.readdirSync(path)
  .map(file => `${path}/${file}`)
  .map(file => file.includes('.') ? file : getFileNames(file));

const flatten = arr => arr.reduce((acc, elem) => acc.concat(Array.isArray(elem) ? flatten(elem) : [elem]), []);

const getWarnings = paths => paths
  .map(file => fs.readFileSync(file, 'utf8')
    .split(/[\n\r]{1,2}/)
    .map(getLineInfo(file))
    .filter(info => info.warnings.length > 0));

const getWarningsText = warnings => warnings.map(warning => `"${warning}"`).join(' and ');

const getWarningDescription = info =>
  `[Warning] ${getWarningsText(info.warnings)} found ` +
  `in '${info.file}' at line ${info.index}: "${info.line}"`;

const filterPaths = pattern => filePaths => filePaths.filter(path => path.includes(pattern));

const toOutput = code => message => Identity({ message, code });

Some(ROOT)
  .map(getFileNames)
  .map(flatten)
  .map(filterPaths(FILE_PATTERN))
  .map(getWarnings)
  .map(flatten)
  .filter(warnings => warnings.length > 0)
  .map(warnings => warnings.map(getWarningDescription).join('\n'))
  .toEither(SUCCESS_MESSAGE)
  .cata(toOutput(0), toOutput(1))
  .forEach(({ message, code }) => {
    console.log(message);
    process.exit(code);
  });
