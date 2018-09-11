import { Map } from 'immutable';

import { AndOperator, BinaryOperator, IsNotOperator, IsOperator, LikeOperator, OrOperator } from '../../ast';
import { BinaryOperatorRuntime } from '../../common/runtimes';
import { SyntaxConfig } from '../../config';
import { and } from './and';
import { is } from './is';
import { isNot } from './is-not';
import { like } from './like';
import { or } from './or';

const getRuntimes = ({ AND, IS, IS_NOT, LIKE, OR }: SyntaxConfig) =>
  Map<BinaryOperator, BinaryOperatorRuntime<any, any, any>>([
    ...AND.map(token => [new AndOperator(token), and]),
    ...IS_NOT.map(token => [new IsNotOperator(token), isNot]),
    ...IS.map(token => [new IsOperator(token), is]),
    ...LIKE.map(token => [new LikeOperator(token), like]),
    ...OR.map(token => [new OrOperator(token), or]),
  ]);

export const getBinaryOperatorRuntime =
  <L, R, V>(operator: BinaryOperator, config: SyntaxConfig): BinaryOperatorRuntime<L, R, V> =>
    getRuntimes(config).get(operator);
