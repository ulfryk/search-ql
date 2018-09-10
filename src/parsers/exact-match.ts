import * as P from 'parsimmon';

import { PhraseExpression } from '../ast';
import { SyntaxConfig } from '../config';

const startEnd = ({ EXACT_MATCHER }: SyntaxConfig) => P.string(EXACT_MATCHER);
const content = ({ EXACT_MATCHER }: SyntaxConfig) => P.regexp(new RegExp(`[^${EXACT_MATCHER}]+`));

const exactMatch = (config: SyntaxConfig) => P.seqMap(
  startEnd(config), content(config), startEnd(config),
  (_start, regExMatches, _end) => regExMatches)
  .map(PhraseExpression.of);

export { exactMatch };
