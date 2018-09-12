import * as P from 'parsimmon';

import { TextExpression } from '../../ast';
import { ParserConfig } from '../../config';
import { exactMatch } from '../exact-match';
import { basicWord } from './basic-word';

export { TextExpression }; // So TSC does not complain

export const basicExpression = (config: ParserConfig) =>
  P.alt(exactMatch(config), basicWord(config));
