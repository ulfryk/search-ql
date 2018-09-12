import * as P from 'parsimmon';

import { TextExpression } from '../../ast';
import { ParserConfig } from '../../config';
import { basicWord } from './basic-word';
import { exactMatch } from './exact-match';

export { TextExpression }; // So TSC does not complain

export const basicExpression = (config: ParserConfig) =>
  P.alt(exactMatch(config), basicWord(config));
