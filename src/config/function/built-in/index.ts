import { anyFunctions } from './any';
import { testFunction } from './test';
import { textFunctions } from './text';

const builtInFunctions = [
  ...anyFunctions,
  ...textFunctions,
  testFunction,
];

export * from './any';
export * from './text';
export {
  anyFunctions,
  builtInFunctions,
  testFunction,
  textFunctions,
};
