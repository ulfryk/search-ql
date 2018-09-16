import { OperatorType } from '../../../common/model';

import { UnaryOperator } from './unary';

export class NotOperator extends UnaryOperator {
  public readonly type = OperatorType.Not;
}
