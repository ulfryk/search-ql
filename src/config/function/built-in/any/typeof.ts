import { List } from 'immutable';
import { None, Right } from 'monet';

import { ValueType } from '../../../../common/model';
import { RequiredFunctionArg } from '../../function-arg';
import { FunctionConfig } from '../../function-config';

export const typeofFunction =
  new FunctionConfig(
    'typeof',
    List([RequiredFunctionArg.fromType(ValueType.Any, 'value')]),
    None(),
    Right(ValueType.Text));
