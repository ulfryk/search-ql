import * as P from 'parsimmon';

import { exactMatch } from '../exact-match';
import { BasicExpression, basicWord } from './basic-word';

export { BasicExpression }; // So TSC does not complain

export const basicExpression = P.alt(exactMatch, basicWord);
