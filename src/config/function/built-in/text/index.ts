import { concatFunction } from './concat';
import { lengthFunction } from './length';
import { lowerFunction } from './lower';
import { trimFunction } from './trim';
import { upperFunction } from './upper';

const textFunctions = [
  concatFunction,
  lengthFunction,
  lowerFunction,
  trimFunction,
  upperFunction,
];

export {
  concatFunction,
  lengthFunction,
  lowerFunction,
  textFunctions,
  trimFunction,
  upperFunction,
};
