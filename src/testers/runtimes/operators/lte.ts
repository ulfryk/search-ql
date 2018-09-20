import { Ordering } from '../../../index';

import { relational } from './relation';

export const lte = relational(ord => [Ordering.Lt, Ordering.Eq].includes(ord));
