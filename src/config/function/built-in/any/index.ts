import { coalesceFunction } from './coalesce';
import { isDateFunction } from './is-date';
import { isEmptyFunction } from './is-empty';
import { isNumberFunction } from './is-number';
import { maxFunction } from './max';
import { minFunction } from './min';
import { typeofFunction } from './typeof';

const anyFunctions = [
  coalesceFunction,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  maxFunction,
  minFunction,
  typeofFunction,
];

export {
  anyFunctions,
  coalesceFunction,
  isDateFunction,
  isEmptyFunction,
  isNumberFunction,
  maxFunction,
  minFunction,
  typeofFunction,
};
