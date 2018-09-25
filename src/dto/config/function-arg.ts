import { ExpressionType, ValueType } from '../../common/model';
import { ArgDemand } from './arg-demand';

export interface IFunctionArg {
  readonly demand: ArgDemand;
  readonly expressionType: ExpressionType[] | null; // non empty array or null
  readonly label: string;
  readonly type: ValueType;
}
