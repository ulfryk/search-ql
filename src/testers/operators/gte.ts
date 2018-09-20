import { Ordering } from '../../common/model';
import { relational } from './relation';

export const gte = relational(ord => [Ordering.Gt, Ordering.Eq].includes(ord));
