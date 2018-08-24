import { Maybe } from 'monet';
import * as P from 'parsimmon';

import { Expression, LabelledExpression } from '../ast';
import { SyntaxConfig } from '../config';
import { basicExpression, basicGroup, matchBasicWord } from './basic';

const label = (config: SyntaxConfig) => P.custom((success, fail) => (input, i) =>
  Maybe.fromNull(input.substring(i).match(config.word))
    .flatMap(([match]) => matchBasicWord(match, config))
    .cata(() => fail(i, 'not a valid label'), ([match]) => success(i + match.length, match)));

const delimiter = (config: SyntaxConfig) => P.regexp(config.delimiter);

const labelledExpression = (config: SyntaxConfig) => P.seqMap(
  label(config),
  delimiter(config),
  P.alt(basicExpression(config), basicGroup(config)),
  (labelValue: string, _s, expression: Expression) =>
    new LabelledExpression(labelValue, expression));

export { label, labelledExpression };
