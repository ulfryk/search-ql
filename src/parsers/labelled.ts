import { Maybe } from 'monet';
import * as P from 'parsimmon';

import { Expression, LabelledExpression } from '../expressions';
import { basicExpression, basicGroup, matchBasicWord } from './basic';
import { delimiter as delimiterMatcher, word } from './common';

const label = P.custom((success, fail) => (input, i) =>
  Maybe.fromNull(input.substring(i).match(word))
    .flatMap(([match]) => matchBasicWord(match))
    .cata(() => fail(i, 'not a valid label'), ([match]) => success(i + match.length, match)));

const delimiter = P.regexp(delimiterMatcher);

const labelledExpression = P.seqMap(
  label,
  delimiter,
  P.alt(basicExpression, basicGroup),
  (labelValue: string, _s, expression: Expression) =>
    new LabelledExpression(labelValue, expression));

export { label, labelledExpression };
