import { anyFunctions } from './any';
import { dateFunctions } from './date';
import { numberFunctions } from './number';
import { testFunction } from './test';
import { textFunctions } from './text';

const builtInFunctions = [
  ...anyFunctions,
  ...dateFunctions,
  ...numberFunctions,
  ...textFunctions,
  testFunction,
];

export * from './any';
export * from './date';
export * from './number';
export * from './text';
export { builtInFunctions, testFunction };
