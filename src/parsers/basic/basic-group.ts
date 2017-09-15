import { Maybe, None, Some } from 'monet';
import * as P from 'parsimmon';

import { Expression } from '../../expressions/expression';
import { fromPairs } from '../../expressions/from-pairs';
import { GROUP_END, GROUP_START, LogicOperator } from '../../syntax-config';
import { logicalOperator } from '../logical-operator';
import { basicExpression } from './basic-expresssion';

const logicalExpression = P.seqMap(
  P.whitespace, logicalOperator, P.whitespace, basicExpression,
  (_s1, operator, _s2, expression) => [Some(operator), expression]);

export const basicGroup = P.seqMap(
  P.string(GROUP_START).skip(P.optWhitespace),
  basicExpression,
  logicalExpression.many(),
  P.optWhitespace.then(P.string(GROUP_END)),
  (_start, head, tail, _end) =>
    [[None<string>(), head]].concat(tail) as [Maybe<LogicOperator>, Expression][])
  .map(fromPairs);
