import { OperatorType } from '../../../config';

import { BinaryOperator } from './binary';

export class OrOperator extends BinaryOperator {
  public readonly type = OperatorType.Or;
}
