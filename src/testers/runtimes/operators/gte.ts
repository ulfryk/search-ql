import { Ordering } from '../../../index';

import { relational } from './relation';

export const gte = relational(ord => [Ordering.Gt, Ordering.Eq].includes(ord));
