import * as P from 'parsimmon';

import { TermExpression, TextExpression } from '../../ast';
import { ParserConfig } from '../../config';
import { exactMatch } from './exact-match';
import { word } from './word';

export { TextExpression }; // So TSC does not complain

export const termExpression = (config: ParserConfig): P.Parser<TermExpression> =>
  P.alt(exactMatch(config), word(config));
