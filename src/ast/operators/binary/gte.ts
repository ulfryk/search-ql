import { OperatorType } from '../../../common/model';
import { RelationalOperator } from './relational';

export class GteOperator extends RelationalOperator {
  public readonly type = OperatorType.Gte;
}
