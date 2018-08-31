import { Map } from 'immutable';

import { AndOperator, BinaryOperator, LikeOperator, OrOperator } from '../../ast';
import { BinaryOperatorRuntime } from '../../common/runtimes';
import { SyntaxConfig } from '../../config';
import { and } from './and';
import { like } from './like';
import { or } from './or';

const getRuntimes = ({ AND, LIKE, OR }: SyntaxConfig) =>
  Map<BinaryOperator, BinaryOperatorRuntime>([
    ...AND.map(token => [new AndOperator(token), and]),
    ...LIKE.map(token => [new LikeOperator(token), like]),
    ...OR.map(token => [new OrOperator(token), or]),
  ]);

export const getBinaryOperatorRuntime =
  (operator: BinaryOperator, config: SyntaxConfig): BinaryOperatorRuntime =>
    getRuntimes(config).get(operator);
