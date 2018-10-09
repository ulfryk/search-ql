import { cleanupFunction } from './cleanup';
import { concatFunction } from './concat';
import { lengthFunction } from './length';
import { lowerFunction } from './lower';
import { removeStopWordsFunction } from './remove-stop-words';
import { trimFunction } from './trim';
import { upperFunction } from './upper';

const textFunctions = [
  cleanupFunction,
  concatFunction,
  lengthFunction,
  lowerFunction,
  removeStopWordsFunction,
  trimFunction,
  upperFunction,
];

export {
  cleanupFunction,
  concatFunction,
  lengthFunction,
  lowerFunction,
  removeStopWordsFunction,
  textFunctions,
  trimFunction,
  upperFunction,
};
