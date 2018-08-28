import { Map } from 'immutable';

import { AndOperator, BinaryOperator, LikeOperator, OrOperator } from '../../ast';
import { SyntaxConfig } from '../../config';
import { and } from './and';
import { BinaryOperatorRuntime } from './binary-operator-runtime';
import { like } from './like';
import { or } from './or';

const getRuntimes = ({ AND, LIKE, OR }: SyntaxConfig) =>
  Map<BinaryOperator, BinaryOperatorRuntime>([
    [new AndOperator(AND), and],
    [new LikeOperator(LIKE), like],
    [new OrOperator(OR), or],
  ]);

export const getBinaryOperatorRuntime =
  (operator: BinaryOperator, config: SyntaxConfig): BinaryOperatorRuntime =>
    getRuntimes(config).get(operator);
