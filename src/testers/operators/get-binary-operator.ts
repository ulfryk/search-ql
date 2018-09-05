import { Map } from 'immutable';

import { AndOperator, BinaryOperator, LikeOperator, OrOperator } from '../../ast';
import { BinaryOperatorRuntime } from '../../common/runtimes';
import { SyntaxConfig } from '../../config';
import { and } from './and';
import { like } from './like';
import { or } from './or';

const getRuntimes = ({ AND, LIKE, OR }: SyntaxConfig) =>
  Map<BinaryOperator, BinaryOperatorRuntime<any, any, any>>([
    ...AND.map(token => [new AndOperator(token), and]),
    ...LIKE.map(token => [new LikeOperator(token), like]),
    ...OR.map(token => [new OrOperator(token), or]),
  ]);

export const getBinaryOperatorRuntime =
  <L, R, V>(operator: BinaryOperator, config: SyntaxConfig): BinaryOperatorRuntime<L, R, V> =>
    getRuntimes(config).get(operator);
