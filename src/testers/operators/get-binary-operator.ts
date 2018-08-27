import { Map } from 'immutable';

import { AndOperator, BinaryOperator, OrOperator } from '../../ast';
import { SyntaxConfig } from '../../config';
import { and } from './and';
import { BinaryOperatorRuntime } from './binary-operator-runtime';
import { or } from './or';

const getRuntimes = ({ AND, OR }: SyntaxConfig) => // LIKE,
  Map<BinaryOperator, BinaryOperatorRuntime>([
    [new AndOperator(AND), and],
    // [new AndOperator(LIKE), like],
    [new OrOperator(OR), or],
  ]);

export const getBinaryOperatorRuntime =
  (operator: BinaryOperator, config: SyntaxConfig): BinaryOperatorRuntime =>
    getRuntimes(config).get(operator);
