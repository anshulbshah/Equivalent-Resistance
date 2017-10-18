#include <iostream>
#include <list>
#include<cstdlib>
#include<vector>
#include<fstream>
#include<math.h>
#include <sstream>
#include<iomanip>
using namespace std;
struct resistance{
    double res;
}resn;
struct nodes{
    int no;
};
struct nodes_dest{
    int no;
    resistance *value;
};
int Nodes,Origin,Dest,N_of_Res,start,endn;
int new_node_counter=0;
double resi,old;
void print_list(vector<int>& vn1,int Nodes,vector< list<nodes_dest> >& adjacencyList)
{
    int done=0;
    for(int i=0;i<Nodes;i++)
    {
        if(vn1[i] != -1)
        {
            list< nodes_dest >::iterator itr = adjacencyList[i].begin();

            while (itr != adjacencyList[i].end()) {
                if(vn1[itr->no - 1]!= -1)
                {
                    done = 1;
                    std::cout << std::fixed << std::setprecision(2) << itr->value->res << std::endl;

                }

                ++itr;
            }
            if(done == 1)
                break;
        }
    }
}

void series_parallel(vector<int>& to_merge,vector< list<nodes_dest> >& adjacencyList,int Nodes,vector<int>& valid_nodes,vector<vector<resistance*> >& v2d2,int &j,resistance *res_point,vector<int>& degree,int &
                     i)
{

        int del_node = to_merge[j];
        //cout<<del_node;
        list< nodes_dest >::iterator itr = adjacencyList[del_node-1].begin();
        int u = 0;
        int node1,node2;
        double res1,res2;
        for(int p=0;p<Nodes;p++)
        {
            if(valid_nodes[itr->no - 1] != -1 && u ==0 )
            {
                node1 = itr->no;
                res1 = itr->value->res;
                u = 1;
            }
            else if(valid_nodes[itr->no - 1] != -1 && u ==1 )
            {
                node2 = itr->no;
                res2 = itr->value->res;
                break;
            }
            ++itr;
        }
        valid_nodes[del_node-1] = -1;
        //cout<<"Node1 : "<<node1<<" Node2 : "<<node2<<endl;
        if(v2d2[node1-1][node2-1]==0)
        {
           // cout<<endl<<"in series";
            v2d2[node1-1][node2-1] = v2d2[del_node-1][node1-1];
            v2d2[node2-1][node1-1] = v2d2[node1-1][node2-1] ;

            nodes_dest new_node;
            new_node.no = node2;
            new_node.value = v2d2[del_node-1][node1-1];


            nodes_dest new_node2;
            new_node2.no = node1;
            new_node2.value = v2d2[del_node-1][node1-1];
            (*v2d2[del_node-1][node1-1]).res = res1+res2;
            v2d2[del_node-1][node1-1] = 0;


            adjacencyList[node1-1].push_back(new_node);
            adjacencyList[node2-1].push_back(new_node2);
        }
        else
        {
            //cout<<endl<<"in parallel";
            old = (*v2d2[node1-1][node2-1]).res;
            (*v2d2[node1-1][node2-1]).res = (old*(res1+res2))/(res1+res2+old);

            degree[node1-1] -= 1;
            degree[node2-1] -= 1;

           // cout<<"degrees"<<degree[node1-1]<<" "<<degree[node2-1]<<endl<<start<<" "<<endn<<endl;
            if(degree[node1-1] == 2 && node1!= start && node1!= endn)
            {
                to_merge[i] = node1;
                i++;
            }
            if(degree[node2-1] == 2 && node2!= start && node2!= endn)
            {
                to_merge[i] = node2;
                i++;
            }
        }
        //cout<<endl;

}

int wheatstone_bridge(vector<int>& to_merge,vector< list<nodes_dest> >& adjacencyList,int Nodes,vector<int>& valid_nodes,vector<vector<resistance*> >& v2d2,int &j,resistance *res_point,vector<int>& degree,int &
                     i,int &m,vector<int>& to_merge3,int &l)
{
    int del_node = to_merge3[m];
    if(valid_nodes[del_node-1]==-1)
        return -1;
    list< nodes_dest >::iterator itr = adjacencyList[del_node-1].begin();
    int w_class = 0;
    vector<int> to_check(Nodes);
    Nodes = Nodes/2;
    int r = 0;
    while (itr != adjacencyList[del_node-1].end())
    {
                if(valid_nodes[itr->no - 1] != -1)
                {
                    to_check[r] = itr->no;
                    r++;
                }
                itr++;
    }
    int p = 1;
    int node1=-1;
    int node2=-1;
    int t=0;
    while (r!=0)
    {

                for(int q=p;q<r;q++)
                {
                    if(v2d2[to_check[t]-1][to_check[q]-1] != 0)
                    {
                        node1 = to_check[t];
                        node2 = to_check[q];
                        //cout<<"node1node2 "<<node1<<" "<<node2;

                        p = -1;
                         break;
                    }
                }
                if(p== -1)
                    break;
                p++;
                t++;
    }
    if(node1!= -1)
    {


       //A wheatstone bridge is found
       //delnode,node1,node2

        double res1 = (*v2d2[node1-1][node2-1]).res;
        double res2 = (*v2d2[node1-1][del_node-1]).res;
        double res3 = (*v2d2[node2-1][del_node-1]).res;

        //Operations on the explored node
        nodes_dest old2new;
        nodes_dest new2old;
        old2new.no = Nodes+new_node_counter+1;
        new2old.no = del_node;
        old2new.value = v2d2[node2-1][node1-1];
        new2old.value = old2new.value;
        v2d2[Nodes+new_node_counter][del_node-1] = v2d2[node2-1][node1-1];
        v2d2[del_node-1][Nodes+new_node_counter] = v2d2[node2-1][node1-1];
        v2d2[node2-1][node1-1] = v2d2[node1-1][node2-1] = 0;
        (*v2d2[del_node-1][Nodes+new_node_counter]).res = (res2*res3)/(res1+res2+res3);
        list< nodes_dest >::iterator itr1 = adjacencyList[del_node-1].begin();
        while(itr1 != adjacencyList[del_node-1].end())
        {
            if(itr1->no == node1 || itr1->no == node2)
            {
                if(adjacencyList[del_node-1].size() == 1)
                {
                    adjacencyList[del_node-1].erase(itr1);
                    break;
                }
                else
                    itr1 = adjacencyList[del_node-1].erase(itr1);

            }
            else
            {
               ++itr1;
            }

        }
        adjacencyList[Nodes+new_node_counter].push_back(new2old);
        adjacencyList[del_node-1].push_back(old2new);


        //Operations for Node 1
        nodes_dest old2new_2;
        nodes_dest new2old_2;
        old2new_2.no = Nodes+new_node_counter+1;
        new2old_2.no = node1;
        old2new_2.value = v2d2[del_node-1][node1-1];
        new2old_2.value = old2new_2.value;
        v2d2[del_node-1][node1-1] = v2d2[node1-1][del_node-1] = 0;
        v2d2[Nodes+new_node_counter][node1-1] = v2d2[node1-1][Nodes+new_node_counter] = old2new_2.value;
        (*v2d2[Nodes+new_node_counter][node1-1]).res = (res2*res1)/(res1+res2+res3);

        itr1 = adjacencyList[node1-1].begin();
        while(itr1 != adjacencyList[node1-1].end())
        {
            if(itr1->no == del_node || itr1->no == node2)
            {
                if(adjacencyList[node1-1].size() == 1)
                {
                    adjacencyList[node1-1].erase(itr1);
                    //cout<<"o1o1";
                    break;
                }
                else
                    itr1 = adjacencyList[node1-1].erase(itr1);
            }
            else
            {
               ++itr1;
            }
        }
        adjacencyList[Nodes+new_node_counter].push_back(new2old_2);
        adjacencyList[node1-1].push_back(old2new_2);

        //Operations for Node 2
        nodes_dest old2new_3;
        nodes_dest new2old_3;
        old2new_3.no = Nodes+new_node_counter +1;
        new2old_3.no = node2;
        old2new_3.value = v2d2[del_node-1][node2-1];
        new2old_3.value = old2new_3.value;
        v2d2[del_node-1][node2-1] = v2d2[node2-1][del_node-1] = 0;
        v2d2[Nodes+new_node_counter][node2-1] = v2d2[node2-1][Nodes+new_node_counter] = old2new_3.value;
        (*v2d2[Nodes+new_node_counter][node2-1]).res = (res3*res1)/(res1+res2+res3);

        itr1 = adjacencyList[node2-1].begin();
        while(itr1 != adjacencyList[node2-1].end())
        {
            if(itr1->no == del_node || itr1->no == node1)
            {
                if(adjacencyList[node2-1].size() == 1)
                {
                    adjacencyList[node2-1].erase(itr1);
                    break;
                }
                else
                    itr1 = adjacencyList[node2-1].erase(itr1);
                //Can make this faster using break .. do if tym
            }
            else
            {
               ++itr1;
            }
        }
        adjacencyList[Nodes+new_node_counter].push_back(new2old_3);
        adjacencyList[node2-1].push_back(old2new_3);



        degree[node1-1] -= 1;
        degree[node2-1] -= 1;
        degree[del_node-1] -=1;
        degree[Nodes+new_node_counter] = 3;
        new_node_counter++;
        if(degree[node1-1] == 2 && node1!= start && node1!= endn)
        {
            to_merge[i] = node1;
            i++;
        }
        if(degree[node1-1] == 3 && node1!= start && node1!= endn)
        {
            to_merge3[l] = node1;
            l++;
        }
        if(degree[node2-1] == 2 && node2!= start && node2!= endn)
        {
            to_merge[i] = node2;
            i++;
        }
        if(degree[node2-1] == 3 && node2!= start && node2!= endn)
        {
            to_merge3[l] = node2;
            l++;
        }
        if(degree[del_node-1] == 2 && del_node!= start && del_node!= endn)
        {
            to_merge[i] = del_node;
            i++;
        }
        if(degree[del_node-1] == 3 && del_node!= start && del_node!= endn)
        {
            to_merge3[l] = del_node;
            l++;
        }
        to_merge3[l] = Nodes + new_node_counter;
        l++;
    }


}
int main(int argc, char* argv[])
{
    string str;
    getline(cin,str);
    string buf;
    stringstream ss(str);
    vector<string> tokens;

    while (ss >> buf)
        tokens.push_back(buf);



    Nodes = atoi(tokens[0].c_str());
    Nodes *= 2;
    N_of_Res = atoi(tokens[1].c_str());
    start = atoi(tokens[2].c_str());
    //cout<<atoi(tokens[2].c_str());
    endn = atoi(tokens[3].c_str());
    vector<int> degree(Nodes);
    vector<int> to_merge(Nodes);
    vector<int> to_merge3(Nodes);
    vector<resistance*> v2(Nodes);
    vector<vector<resistance*> > v2d2(Nodes,v2);
    vector< list<nodes_dest> > adjacencyList(Nodes+1);
    resistance * res_point = new resistance[N_of_Res];

    for(int i=0;i<N_of_Res;i++)
    {
        string str1;
        getline(cin,str1);
        stringstream ss1(str1);
        vector<string> tokens1;
        while (ss1 >> buf)
            tokens1.push_back(buf);

        Origin = atoi(tokens1[0].c_str());
        Dest = atoi(tokens1[1].c_str());
        resi = atof(tokens1[2].c_str());

        nodes_dest for_to;
        nodes_dest for_from;

        for_to.no = Dest;
        for_to.value = &res_point[i];

        for_from.no = Origin;
        for_from.value = &res_point[i];

        if(v2d2[Origin-1][Dest-1]==0)
        {
            //cout<<"is 0"<<endl;
            v2d2[Origin-1][Dest-1] = &res_point[i];
            v2d2[Dest-1][Origin-1] = &res_point[i];
            res_point[i].res = resi;
            //cout<<"resistance "<<i+1<<resi<<endl;
            adjacencyList[Origin-1].push_back(for_to);
            adjacencyList[Dest-1].push_back(for_from);
            degree[Origin-1]++;
            degree[Dest-1]++;
        }
        else
        {
            //cout<<"was here";
            old = (*v2d2[Origin-1][Dest-1]).res;
            (*v2d2[Origin-1][Dest-1]).res = (old*resi)/(resi+old);
        }




        //cout<<"Done";
    }

    int i,j,l;
    for(i=0,j=0,l=0;j<Nodes;j++)
    {
        if(degree[j] == 2 && j!=start-1 && j!=endn-1)
        {
            to_merge[i] = j+1;
            i++;
            //cout<<"IN To merge with node "<<j+1<<endl;
        }
        if(degree[j] >= 3 && j!=start-1 && j!=endn-1 )
        {
            to_merge3[l] = j+1;
            l++;
        }

    }
    j=0;
    int m=0;
    //cout<<"left to merge";
    vector<int> valid_nodes(Nodes);
    int iv=0;
    while(1==1)
    {
        //to_merge keeps track of necessary series parallel operations
        //to_merge3 for wheatstone bridge operations
//        if(to_merge[j] == 0)
//            break;
       // cout<<"degree of 7: "<<degree[6]<<" ";
        if(to_merge[j] == 0 && to_merge3[m] == 0)
        {
             break;
        }
        else if(to_merge[j] != 0)
        {
            if(degree[to_merge[j]-1] == 2)
            {
               // cout<<"In series-parallel ";
                series_parallel(to_merge,adjacencyList,Nodes,valid_nodes,v2d2,j,res_point,degree,i);
                for(int y=0;y<i;y++)
                {
                   // cout<<to_merge[y]<<" ";
                }
               // cout<<endl;
                for(int y=0;y<l+1;y++)
                {
                   // cout<<to_merge3[y]<<" ";
                }
              //  cout<<endl;
            }
            j++;
        }
        else if((to_merge3[m]) != 0)
        {
            if(degree[to_merge3[m]-1] >= 3)
            {
              //  cout<<"In wheat stone bridge ";
                iv = wheatstone_bridge(to_merge,adjacencyList,Nodes,valid_nodes,v2d2,j,res_point,degree,i,m,to_merge3,l);
            }
            m++;
        }


    }

    //cout<<"Answer"<<endl;
    print_list(valid_nodes,Nodes,adjacencyList);


    return 0;
}


