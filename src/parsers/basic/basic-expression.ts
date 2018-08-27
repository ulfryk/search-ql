import * as P from 'parsimmon';

import { TextExpression } from '../../ast';
import { SyntaxConfig } from '../../config';
import { exactMatch } from '../exact-match';
import { basicWord } from './basic-word';

export { TextExpression }; // So TSC does not complain

export const basicExpression = (config: SyntaxConfig) =>
  P.alt(exactMatch(config), basicWord(config));
