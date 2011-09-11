class #{module} < ApplicationController
  before_filter :make_#{resource}, only: [:new, :create]
  before_filter :load_#{resource}, only: [:show, :edit, :update, :destroy]

  def index
    @#{resources} = #{model}.page(1).per_page(20)
  end

  def new
  end

  def create
    if @#{resource}.update_attributes(params[:#{resource}])
      flash_notice
      redirect_to #{resource}_path(@#{resource})
    else
      flash_error
      render :new
    end
  end

  def show
  end

  def edit
  end

  def update
    if @#{resource}.update_attributes(params[:#{resource}])
      flash_notice
      redirect_to #{resource}_path(@#{resource})
    else
      flash_error
      render :new
    end
  end

  def destroy
    @#{resource}.destroy
    flash_notice
    redirect_to #{resources}_path
  end

  private

  def make_#{resource}
    @#{resource} = #{model}.new
  end

  def load_#{resource}
    @#{resource} = #{model}.find_by_id(params[:id])
  end
end
