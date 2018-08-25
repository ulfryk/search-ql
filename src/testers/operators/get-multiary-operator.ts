import { Map } from 'immutable';

import { AndOperator, MultiaryOperator, OrOperator } from '../../ast';
import { SyntaxConfig } from '../../config';
import { and } from './and';
import { OperatorRuntime } from './operator-runtime';
import { or } from './or';

const getMultiaryRuntimes = ({ AND, OR }: SyntaxConfig) => Map<MultiaryOperator, OperatorRuntime>([
  [new AndOperator(AND), and],
  [new OrOperator(OR), or],
]);

export const getMultiaryOperatorRuntime =
  (operator: MultiaryOperator, config: SyntaxConfig): OperatorRuntime =>
    getMultiaryRuntimes(config).get(operator);
