/* tslint:disable:max-classes-per-file */
import { ValueType } from '../../common/model';
import { ArgDemand, IFunctionArg } from '../../dto';

abstract class FunctionArg {

  public static fromJS(_pojo: IFunctionArg): FunctionArg {
    throw Error('unimplemented');
  }

  constructor(
    public readonly type: ValueType,
    public readonly label: string, // consider Maybe<string>
  ) {}

  public abstract toJS(): IFunctionArg;

}

class OptionalFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string) {
    return new OptionalFunctionArg(type, label);
  }

  public toJS() {
    return {
      demand: ArgDemand.Optional,
      label: this.label,
      type: this.type,
    };
  }

}

class RequiredFunctionArg extends FunctionArg {

  public static fromType(type: ValueType, label: string) {
    return new RequiredFunctionArg(type, label);
  }

  public toJS() {
    return {
      demand: ArgDemand.Required,
      label: this.label,
      type: this.type,
    };
  }

}

FunctionArg.fromJS = ({ demand, label, type }: IFunctionArg) => {
  switch (demand) {
    case ArgDemand.Optional: return OptionalFunctionArg.fromType(type, label);
    case ArgDemand.Required: return RequiredFunctionArg.fromType(type, label);
    default: throw Error(`No such AegDemand value: ${demand}`);
  }
};

export { FunctionArg, OptionalFunctionArg, RequiredFunctionArg };
