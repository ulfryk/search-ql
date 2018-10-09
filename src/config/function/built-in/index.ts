import { anyFunctions } from './any';
import { numberFunctions } from './number';
import { testFunction } from './test';
import { textFunctions } from './text';

const builtInFunctions = [
  ...anyFunctions,
  ...numberFunctions,
  ...textFunctions,
  testFunction,
];

export * from './any';
export * from './number';
export * from './text';
export { builtInFunctions, testFunction };
