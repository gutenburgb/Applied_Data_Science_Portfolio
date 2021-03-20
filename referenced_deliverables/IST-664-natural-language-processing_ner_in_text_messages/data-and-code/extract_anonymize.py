"""
SMS Entity Analysis
By Leland Ball, December 2020

conda install pyprind


Parses large (~3GB) xml file into:
    - labels.csv

<smses count="141155" backup_set="fd109953-87c2-426e-8ebd-xxxxxxxxxx"
    backup_date="1607962211786" type="full">

    <sms protocol="0" address="xxxxxxx" date="1359309433199" type="2" subject="null"
        body="Rocklands?" toa="null" sc_toa="null" service_center="null" read="1" status="-1"
        locked="0" date_sent="0" sub_id="-1" readable_date="Jan 27, 2013 12:57:13 PM"
        contact_name="xxxxxxxxxx" />

    <sms protocol="0" address="xxxxxxx" date="1359309556507" type="2"
        subject="null" body="We are arriving at it, if you want to join us" toa="null"
        sc_toa="null" service_center="null" read="1" status="-1" locked="0" date_sent="0"
        sub_id="-1" readable_date="Jan 27, 2013 12:59:16 PM" contact_name="xxxxxxxxxx" />

    <mms date="1359688178000" rr="129" sub="null" ct_t="application/vnd.wap.multipart.related"
        read_status="null" seen="1" msg_box="2" address="xxxxxxx" sub_cs="null"
        resp_st="128" retr_st="null" d_tm="null" text_only="0" exp="604800" locked="0"
        m_id="null" st="null" retr_txt_cs="null" retr_txt="null"
        creator="com.riteshsahu.SMSBackupRestore" date_sent="0" read="1" m_size="null"
        rpt_a="null" ct_cls="null" pri="129" sub_id="-1" style_code="0" tr_id="xxxxxxxxxx"
        resp_txt="null" ct_l="null" m_cls="personal" d_rpt="129" v="18" m_type="128"
        phone_id="-1" readable_date="Jan 31, 2013 10:09:38 PM" contact_name="Leland">

        <part seq="0" ct="image/jpeg" name="null" chset="null" cd="null" fn="null"
        cid="&lt;PART_1359688166714&gt;" cl="PART_1359688166714" ctt_s="null" ctt_t="null"
        text="null"
        data="/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAYEBQYFBAYGBQYHBwYIChAKCgkJChQO..."

NOTES:
    Group mms text names are separated by: ", "
    Group mms text numbers are separted by: "~"

"""


from pathlib import Path
import xml.etree.ElementTree as ET
import csv
import re
import pyprind
import sys  # for pyprind

MY_PHONE = '509######'  # normalized, non-country-code number (9 digit) to treat as 'root'

def write_csv(lines, path_csv, header=None):
    path_csv = Path(path_csv)

    if not header and len(lines) > 0:
        header = lines[0].keys()

    try:
        with path_csv.open('w', encoding="utf-8", newline='') as csvfile:  # 'w', encoding="utf-8", newline=''
            writer = csv.DictWriter(csvfile, fieldnames=header)
            writer.writeheader()
            for data in lines:
                writer.writerow(data)
    except IOError:
        print("I/O error")

def dedupe_dicts(lines, remove_nones=True):
    """
    Takes a list of dictionaries, and dedupes by all keys within them
    :param lines: list containing dictionaries
    :return: Returns a deduped list of dicts
    """
    visited = set()
    new_list = []
    for d in lines:
        t = tuple(d.items())
        if t not in visited:
            visited.add(t)
            if remove_nones:
                if not None in d.values():
                    new_list.append(d)
            else:
                new_list.append(d)
    return new_list

def dedupe_labels(labels):
    """
    takes a list of dicts, dedupes on 'number' and prefers longer 'name' key
    :param labels: list of dicts
    :return: deduped list of dicts
    """
    new_labels = {}
    for d in labels:
        num = d['number']
        name = d['name']
        prior_name = new_labels.get(num, '')
        if len(name) > len(prior_name):
            new_labels[num] = name
    return new_labels

def normalize_phone(digits_str):
    digits_clean = re.sub('\D', '', digits_str)
    digits_cleaner = digits_clean[-10:]
    return '0000' if len(digits_cleaner) < 7 else digits_cleaner

def normalize_name(name):
    name_clean = re.sub(' H$| [hH]inge$| X$| BB$| CMB$| EH$|[(]\w+!*[)]|["]\w+["]', '', name, flags=re.MULTILINE)
    return str(name_clean).strip()

def update_name_dict(name_dict, names, numbers):
    for name, num in zip(names, numbers):
        name = normalize_name(name)
        num = normalize_phone(num)
        s = name_dict.get(num, set())
        s.update((name,))
        name_dict[num] = s
    return name_dict

def in_any_part_of(s, l):
    l = list(l)
    return any([True for x in l if s in normalize_phone(x)])

def extract(path_xml_file_import, path_csv_file_export, path_csv_contact_labels):
    """
    Imports SMS and MMS messages from XML file and exports new XML file with:
        - sms and mms converted to "msg"
        - mms data stripped (no "part" tags)
        - unknown tags are removed
        - contacts names removed and placed in separate labels csv

    :param path_xml_file_import: pathlib PATH object (or string path) of .xml file to import
    :param path_xml_file_export: pathlib PATH object (or string path) of new .xml file to export
    :param path_contact_labels: pathlib PATH object (or string path) of extracted contacts labels
    :return: void
    """
    path_xml_file_import = Path(path_xml_file_import)
    path_csv_file_export = Path(path_csv_file_export)
    path_csv_contact_labels = Path(path_csv_contact_labels)

    number_name = {}

    context = ET.iterparse(path_xml_file_import, events=("start", "end"))
    is_first = True
    lines = []
    total_entries = 10  # will be updated upon first element
    bar = None
    actual_count = 0
    for event, elem in context:
        # get the root element
        if is_first:
            root = elem
            is_first = False
            total_entries = int( int(root.attrib['count']) * 3.5225 )  # empirically determined
            bar = pyprind.ProgBar(total_entries, stream=sys.stdout)

        if event == 'end':
            actual_count += 1
            d = {
                'date': '',
                'type': '',  # one of 'sms', 'mms'
                'address': '',
                'image': False,
                'body': '',
                'contact_name': ''
            }

            if elem.tag == 'sms':
                d['date'] = elem.attrib['date']
                d['direction'] = 'INBOUND' if elem.attrib['type'] == "1" else 'OUTBOUND'
                d['type'] = elem.tag
                d['address'] = tuple([normalize_phone(elem.attrib['address'])])
                d['body'] = elem.attrib['body'] if 'body' in elem.attrib.keys() else ''
                d['contact_name'] = tuple([normalize_name(elem.attrib['contact_name'])])

                lines.append(d)

                update_name_dict(number_name, [elem.attrib['contact_name']], [elem.attrib['address']])

            if elem.tag == 'mms':
                date = elem.attrib['date']
                # crawl through 'parts' tag, for each part
                last_seq = '-1'
                text_glom = ''
                parts = elem.find('parts')
                if parts != None:
                    for part in parts.findall('part'):
                        last_seq = part.attrib['seq']
                        if last_seq != '-1':
                            text_glom += part.attrib['text'] + ' '

                direction = 'INBOUND'
                addrs = elem.find('addrs')
                address_set = set()
                if addrs:
                    for addr in addrs.findall('addr'):
                        if addr.attrib['type'] == '137' and MY_PHONE in normalize_phone(addr.attrib['address']):  # in_any_part_of(MY_PHONE, addr.attrib['address']):
                            direction = 'OUTBOUND'
                        address_set.update((addr.attrib['address'],))
                else:
                    direction = 'UNK'

                if len(text_glom) < 10 and 'null' in text_glom:
                    text_glom = ''
                    d['image'] = True

                d['date'] = date
                d['direction'] = direction
                d['type'] = elem.tag
                d['address'] = tuple([normalize_phone(a) for a in elem.attrib['address'].split('~')])
                d['body'] = text_glom
                d['contact_name'] = tuple([normalize_name(n) for n in elem.attrib['contact_name'].split(',')])

                lines.append(d)

                update_name_dict(number_name,
                                 ['' for x in address_set],  # these contacts must be added later
                                 list(address_set))
            if bar:
                bar.update()

    # for some reason there are obvious dupes in the texts. Remove them
    cleaned_lines = dedupe_dicts(lines)

    # convert dictionary-of-sets to dict with IDs (basically a list)
    write_csv(cleaned_lines, path_csv_file_export)

    # convert labels to the right format (list of dicts)
    labels = []
    for num,names in number_name.items():
        for name in names:
            labels.append({'number': num, 'name': name})
    # also dedupe the contacts (prefer entries with name or longer name)
    labels_deduped = dedupe_labels(labels)
    # and back again
    labels = []
    for num,name in labels_deduped.items():
        labels.append({'number': num, 'name': name})

    # write the labels to a file
    write_csv(labels, path_csv_contact_labels)

