import { Ordering } from '../../common/model';
import { relational } from './relation';

export const gt = relational(ord => ord === Ordering.Gt);
