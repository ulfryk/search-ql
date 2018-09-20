import { Ordering } from '../../../index';

import { relational } from './relation';

export const gt = relational(ord => ord === Ordering.Gt);
