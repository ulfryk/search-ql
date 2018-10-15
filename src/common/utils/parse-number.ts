import Big from 'big.js';

import { isNumber } from './is-number';

export const parseNumber = (value: string): Big => {
  const trimmed = value.trim();

  try {
    return Big(trimmed);
  } catch (e) {
    if (isNumber(trimmed)) {
      return Big(Number(trimmed));
    }

    throw e;
  }
};
