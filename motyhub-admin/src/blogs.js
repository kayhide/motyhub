import React from 'react';
import { List, Datagrid, TextField, Filter, TextInput } from 'admin-on-rest';

export const BlogList = (props) => (
    <List {...props} filters={<BlogFilter />}>
        <Datagrid>
        <TextField source="id" />
        <TextField source="title" />
        <TextField source="url" />
        </Datagrid>
    </List>
);


const BlogFilter = (props) => (
    <Filter {...props}>
        <TextInput label="url" source="url" />
        <TextInput label="title" source="title" />
    </Filter>
);
