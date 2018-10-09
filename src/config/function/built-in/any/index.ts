import { coalesceFunction } from './coalesce';
import { isDateFunction } from './is-date';
import { isEmptyFunction } from './is-empty';
import { isNumberFunction } from './is-number';
import { lengthFunction } from './length';
import { typeofFunction } from './typeof';

const anyFunctions = [
  coalesceFunction,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  lengthFunction,
  typeofFunction,
];

export {
  anyFunctions,
  coalesceFunction,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  lengthFunction,
  typeofFunction,
};
