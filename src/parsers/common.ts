import { EXACT_MATCHER, GROUP_END, GROUP_START, LABEL_DELIMITER } from '../syntax-config';

const restricted = [EXACT_MATCHER, GROUP_END, GROUP_START, LABEL_DELIMITER, 's']
  .map(sign => `\\${sign}`)
  .join('');

export const restrictedSigns = new RegExp(`^[^${restricted}]+`);

export const word = /^[\w_\.]+/;

export const delimiter = new RegExp(`\\s*${LABEL_DELIMITER}\\s*`);
