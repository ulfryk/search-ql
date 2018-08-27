import { Maybe, None, Some } from 'monet';
import * as P from 'parsimmon';

import { Expression, fromPairs } from '../../ast';
import { SyntaxConfig } from '../../config';
import { binaryOperator } from '../binary-operator';
import { basicExpression } from './basic-expression';

const binaryOperationExpression = (config: SyntaxConfig) => P.seqMap(
  P.whitespace, binaryOperator(config), P.whitespace, basicExpression(config),
  (_s1, operator, _s2, expression) => [Some(operator), expression]);

export const basicGroup = (config: SyntaxConfig) => P.seqMap(
  P.string(config.GROUP_START).skip(P.optWhitespace),
  basicExpression(config),
  binaryOperationExpression(config).many(),
  P.optWhitespace.then(P.string(config.GROUP_END)),
  (_start, head, tail, _end) =>
    [[None<string>(), head]].concat(tail) as [Maybe<string>, Expression][])
  .map(parser => fromPairs(parser, config));
