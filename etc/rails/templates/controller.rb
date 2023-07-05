class #{module} < ApplicationController
  before_action :make_#{resource}, only: [:new, :create]
  before_action :load_#{resource}, only: [:show, :edit, :update, :destroy]

  def index
    @#{resources} = #{model}.page(1).per(20)
  end

  def new
  end

  def create
    if @#{resource}.update(#{resource}_params)
      flash_notice
      redirect_to #{resource}_path(@#{resource})
    else
      flash_error
      render :new, status: 422
    end
  end

  def show
  end

  def edit
  end

  def update
    if @#{resource}.update_attributes(#{resource}_params)
      flash_notice
      redirect_to #{resource}_path(@#{resource})
    else
      flash_error
      render :edit, status: 422
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
    @#{resource} = #{model}.find_by_id(params[:id]) or
      render status: 404, text: 'Not Found'
  end

  def #{resource}_params
    params.require(:#{resource}).permit(...)
  end
end
