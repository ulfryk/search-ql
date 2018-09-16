import * as P from 'parsimmon';

import { fromMatch, TermExpression } from '../../ast';
import { ParserConfig } from '../../config';
import { matchTerm } from './match-term';

export const word = (config: ParserConfig): P.Parser<TermExpression> =>
  P.custom<string>((success, fail) => (input, i) =>
    matchTerm(input.substring(i), config)
      .map(([match]) => match)
      .cata(() => fail(i, 'not a valid word'), match => success(i + match.length, match)))
    .map(fromMatch(config));
