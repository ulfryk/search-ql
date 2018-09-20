import { Ordering } from '../../../index';

import { relational } from './relation';

export const lt = relational(ord => ord === Ordering.Lt);
