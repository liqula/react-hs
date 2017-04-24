/* As part of each store, we have a javascript object with two properties:

sdata: holds a value of type @Export storeData@ which is the current data for the store

views: an array of @setState@ functions for component views.  The component views
    add and remove from this property directly inside their lifecycle methods.
*/
var hsreact$storedata = {};

function hsreact$transform_new_store(store, newData) {  // FIXME: rename to update_new_store
    var oldD = store.sdata;
    store.sdata = newData;
    h$release(oldD);
    for (var storeTy in store.views) {
        store.views[storeTy](store.sdata);
    }
}

var hsreact$viewCntr = 0;
function hsreact$register_view(store, f) {
    var cntr = hsreact$viewCntr++;
    store.views[cntr] = f;
    return cntr;
}

function hsreact$clear_view(store, cntr) {
    delete store.views[cntr];
}
