import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const lengthFunction =
  new FunctionConfig(
    'length',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'value')]),
    None(),
    Right(ValueType.Number));
