import * as P from 'parsimmon';

import { TermExpression } from '../../ast';
import { SyntaxConfig } from '../../config';
import { matchBasicWord } from './match-basic-word';

export { TermExpression }; // So TSC does not complain

export const basicWord = (config: SyntaxConfig) => P.custom<string>((success, fail) => (input, i) =>
  matchBasicWord(input.substring(i), config)
    .map(([match]) => match)
    .cata(() => fail(i, 'not a valid word'), match => success(i + match.length, match)))
  .map(TermExpression.of);
