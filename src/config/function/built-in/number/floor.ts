import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const floorFunction =
  new FunctionConfig(
    'floor',
    List([RequiredFunctionArg.fromType(ValueType.Number, 'value')]),
    None(),
    Right(ValueType.Number));
