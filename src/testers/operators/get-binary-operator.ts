import { Map } from 'immutable';

import { AndOperator, BinaryOperator, LikeOperator, OrOperator } from '../../ast';
import { SyntaxConfig } from '../../config';
import { and } from './and';
import { BinaryOperatorRuntime } from './binary-operator-runtime';
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
