import { Ordering } from '../../common/model';
import { relational } from './relation';

export const lte = relational(ord => [Ordering.Lt, Ordering.Eq].includes(ord));
