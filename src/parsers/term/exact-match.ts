import * as P from 'parsimmon';

import { PhraseExpression, SelectorExpression, TermExpression } from '../../ast';
import { ParserConfig } from '../../config';

const startEnd = ({ EXACT_MATCHER }: ParserConfig) => P.string(EXACT_MATCHER);
const content = ({ EXACT_MATCHER }: ParserConfig) => P.regexp(new RegExp(`[^${EXACT_MATCHER}]+`));

const exactMatch = (config: ParserConfig): P.Parser<TermExpression> =>
  P.seqMap(
      startEnd(config), content(config), startEnd(config),
      (_start, regExMatches, _end) => regExMatches)
    .map(PhraseExpression.of)
    .map(SelectorExpression.fromTerm(config.model));

export { exactMatch };
