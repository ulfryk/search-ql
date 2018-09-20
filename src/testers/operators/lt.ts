import { Ordering } from '../../common/model';
import { relational } from './relation';

export const lt = relational(ord => ord === Ordering.Lt);
