import { OperatorType } from '../common/model';

export interface IOperator {
  readonly token: string;
  readonly type: OperatorType;
}
