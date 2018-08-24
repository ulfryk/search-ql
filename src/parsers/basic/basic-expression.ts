import * as P from 'parsimmon';

import { SyntaxConfig } from '../../config';
import { exactMatch } from '../exact-match';
import { BasicExpression, basicWord } from './basic-word';

export { BasicExpression }; // So TSC does not complain

export const basicExpression = (config: SyntaxConfig) =>
  P.alt(exactMatch(config), basicWord(config));
