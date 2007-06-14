class ExampleController < ApplicationController
  def index
    list
    render :action => 'list'
  end

  # GETs should be safe (see http://www.w3.org/2001/tag/doc/whenToUseGet.html)
  verify :method => :post, :only => [ :destroy, :create, :update ],
         :redirect_to => { :action => :list }

  def list
    @example_pages, @examples = paginate :examples, :per_page => 10
  end

  def show
    @example = Example.find(params[:id])
  end

  def new
    #@example = Example.new
  end

  def create
    #@example = Example.new(params[:example])
    #if @example.save
    #  flash[:notice] = 'Example was successfully created.'
    #  redirect_to :action => 'list'
    #else
    #  render :action => 'new'
    #end
  end

  def edit
    #@example = Example.find(params[:id])
  end

  def update
    #@example = Example.find(params[:id])
    #if @example.update_attributes(params[:example])
    #  flash[:notice] = 'Example was successfully updated.'
    #  redirect_to :action => 'show', :id => @example
    #else
    #  render :action => 'edit'
    #end
  end

  def destroy
    #Example.find(params[:id]).destroy
    #redirect_to :action => 'list'
  end
  
  def save
    edata = params[:example]
    example = Example.find(:first,
      :conditions => ["filename = ?", edata[:filename]])
    if example && example.passwd && example.passwd != edata[:passwd]
      @msg = 'Incorrect password'
    else
      if example == nil
        example = Example.new(edata)
      else
        example.filename = edata[:filename]
        example.title = edata[:title]
        example.agent = params[:agent]
        example.passwd = edata[:passwd]
      end 
      example.save!
      Rule.delete_all ["eid = ?", example.id]
      redexes = params[:redex]
      reacts = params[:react]
      insts = params[:inst]
      for i in 0...redexes.length
        rule = Rule.find(:first,
          :conditions => ["eid = ? and ruleno = ?", example.id, i])
        rule = Rule.new() unless rule
        rule.ruleno = i
        rule.eid = example.id
        rule.redex = redexes[i.to_s] 
        rule.react = reacts[i.to_s] 
        rule.inst = insts[i.to_s] 
        rule.save!
      end
      @msg = 'Example saved as ' + example.filename
    end
    @example_pages, @examples =
      paginate :examples, {:per_page => 30, :order => "filename"}
    render :partial => "examples"
  end
  
  def delete
    edata = params[:example]
    example = Example.find(:first,
      :conditions => ["filename = ?", edata[:filename]])
    if example
      if example.passwd && example.passwd != edata[:passwd]
        @msg = 'Incorrect password'
      else
        Rule.delete_all ["eid = ?", example.id]
        example.destroy
        @msg = 'Deleted ' + example.filename
      end
    else
      @msg = ''
    end
    @example_pages, @examples =
      paginate :examples, {:per_page => 30, :order => "filename"}
    render :partial => "examples"
  end
end
