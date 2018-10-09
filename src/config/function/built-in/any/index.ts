import { coalesceFunction } from './coalesce';
import { isDateFunction } from './is-date';
import { isEmptyFunction } from './is-empty';
import { isNumberFunction } from './is-number';
import { typeofFunction } from './typeof';

const anyFunctions = [
  coalesceFunction,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  typeofFunction,
];

export {
  anyFunctions,
  coalesceFunction,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  typeofFunction,
};
