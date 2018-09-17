import { Map } from 'immutable';

import { AndOperator, BinaryOperator, IsNotOperator, IsOperator, LikeOperator, NotLikeOperator, OrOperator } from '../../ast';
import { BinaryOperatorRuntime } from '../../common/runtimes';
import { ParserConfig } from '../../config';
import { and } from './and';
import { is } from './is';
import { isNot } from './is-not';
import { like } from './like';
import { notLike } from './not-like';
import { or } from './or';

const getRuntimes = ({ AND, IS, IS_NOT, LIKE, NOT_LIKE, OR }: ParserConfig) =>
  Map<BinaryOperator, BinaryOperatorRuntime<any, any, any>>([
    ...AND.map(token => [new AndOperator(token), and]),
    ...IS_NOT.map(token => [new IsNotOperator(token), isNot]),
    ...IS.map(token => [new IsOperator(token), is]),
    ...LIKE.map(token => [new LikeOperator(token), like]),
    ...NOT_LIKE.map(token => [new NotLikeOperator(token), notLike]),
    ...OR.map(token => [new OrOperator(token), or]),
  ]);

export const getBinaryOperatorRuntime =
  <L, R, V>(operator: BinaryOperator, config: ParserConfig): BinaryOperatorRuntime<L, R, V> =>
    getRuntimes(config).get(operator);
