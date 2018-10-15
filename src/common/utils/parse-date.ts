import { parse } from 'chrono-node';

import { TimeFrame } from '../model';

// TODO: Drop wrong parsing results
export const parseDate = (value: string): TimeFrame => TimeFrame.fromChrono(parse(value), value);
