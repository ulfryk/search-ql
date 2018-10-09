import { anyFunctions } from './any';
import { testFunction } from './test';

const builtInFunctions = [
  ...anyFunctions,
  testFunction,
];

export * from './any';
export {
  anyFunctions,
  builtInFunctions,
  testFunction,
};
