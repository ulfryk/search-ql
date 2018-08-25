import { OperatorType } from '../../config';

import { MultiaryOperator } from './multiary-operator';

export class OrOperator extends MultiaryOperator {
  public static readonly one = new OrOperator();
  public readonly type = OperatorType.Or;
}
