import { ValueType } from '../../common/model';
import { ArgDemand } from './arg-demand';

export interface IFunctionArg {
  readonly demand: ArgDemand;
  readonly label: string;
  readonly type: ValueType;
}
