import { coalesceFunction } from './coalesce';
import { isDateFunction } from './is-date';
import { isEmptyFunction } from './is-empty';
import { isNumberFunction } from './is-number';
import { lengthFunction } from './length';
import { testFunction } from './test';
import { typeofFunction } from './typeof';

const builtInFunctions = [
  coalesceFunction,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  lengthFunction,
  testFunction,
  typeofFunction,
];

export {
  coalesceFunction,
  builtInFunctions,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  lengthFunction,
  testFunction,
  typeofFunction,
};
