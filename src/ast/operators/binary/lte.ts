import { OperatorType } from '../../../common/model';
import { RelationalOperator } from './relational';

export class LteOperator extends RelationalOperator {
  public readonly type = OperatorType.Lte;
}
